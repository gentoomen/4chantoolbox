/*
 *  4chantoolbox - Educational Yotsuba scripts and tools.
 *  Copyright 2011 gentoomen. All rights reserved.
 *  This file is distributed under the terms of the Do What The Fuck You Want To Public License, Version 2. 
 *  	Or, at the discretion of the project, any comparable license chosen.
 * 
 *  Scraper-ObjC.m - Canonical FoundationKit image fetcher.
 */

#import <Cocoa/Cocoa.h>

int main (int argc, const char * argv[]) {
	// An AutoreleasePool should be set up manually for processes that have their own single run loop; in Cocoa GUI programs this is managed by the system.
	// This will automatically take care of any autoreleased objects instantiated within its scope.
	NSAutoreleasePool* pool = [[NSAutoreleasePool alloc] init];
	
	// The standardUserDefaults singleton contains all preferences associated with the running program, including command line arguments
	// However, this is limited to args in the form "-key value". Usually a fair trade-off since it's so much nicer than getopt.
	NSUserDefaults* args = [NSUserDefaults standardUserDefaults];
	// Normally to check for the presence of command line arguments we'd do the following
	//		[[args volatileDomainForName:NSArgumentDomain] count]
	// However in our case we can't do anything without -u being set anyway, so we should just bail out if that isn't present.
	if(![args objectForKey:@"u"]) {
	    NSLog( @"\nObjective-C sample application to scrape 4chan, part of 4chantoolbox.\n"
				"\n"
				"OPTIONS\n"
				" -u <url>\t url to download\n"
				" -o <path>\t output dir [./]\n"
				" -t <int>\t thread refresh in seconds [10]\n"
				"\n");	
		[pool drain];
		return 0;
	}
	
	// Now we can just ask the userdefaults for a string associated with flag "u"
	NSURL* url = [NSURL URLWithString:[args stringForKey:@"u"]];
	NSString* output;
	if([args objectForKey:@"o"])
		// stringByStandardizingPath is a great method that sanitizes a path by expanding tildes, resolving symlinks, compacting extraneous ./, etc
		output = [[args stringForKey:@"o"] stringByStandardizingPath];
	else 
		// No directory provided, default is cwd
		output = [[NSFileManager defaultManager] currentDirectoryPath];
	NSInteger refresh = 10;
	if([args objectForKey:@"t"])
		refresh = [args integerForKey:@"t"];

	NSLog(@"Downloading %@",url);
	NSLog(@"Saving to location %@",output);
	if(refresh)	NSLog(@"Timer set to every %d seconds.",refresh);

	NSXMLDocument* thread;
	NSUInteger lastindex = 0; // Don't bother with images that have already been processed
	NSError* e = nil; // If something goes wrong with the thread this will tell us the specific error
	NSUInteger downtot = 0;
	do {
		// It's a good idea to create an inner autorelease pool for loops where many objects may be allocated; this keeps our memory footprint as small as possible
		NSAutoreleasePool* loop = [[NSAutoreleasePool alloc] init];
		NSDate* lasttime = [NSDate date];
		// NSXMLDocument provides a full XML DOM parser with tidying support and XPath/XQuery methods which we'll use to get the elements we want
		thread = [[NSXMLDocument alloc] initWithContentsOfURL:url options:NSXMLDocumentTidyHTML error:&e];
		// if(thread != nil) if you prefer, but nil is always boolean false
		if(thread)
			NSLog(@"%@ successfully fetched.",url);
		else
			NSLog(@"Failed to fetch %@, error: %@",url,[e localizedDescription]);

		// This XPath will get us all of the post image URLs, here's the breakdown:
		//	/html/body/form 	The entire thread is below this node
		//	             //		Abitrary depth selector. For efficiency it's best to avoid this and use full paths where possible, however in this case
		//				 OP nodes are directly under /html/body/form, while replies are within /html/body/form/table/tr/td[@class="reply"]
		//	  a[img[@md5]]/ 	Selects all <a> that contain <img> tags, and furthermore only where the imgs have md5 attributes (otherwise we get a JList ad too)
		//	          @href		The final content that will be selected will be the string content of the image link's href attribute 
		NSArray* images = [thread nodesForXPath:@"/html/body/form//a[img[@md5]]/@href" error:NULL];
		[thread release];
		// This if could be avoided if we weren't tracking a range that might no longer exist
		if([images count] > lastindex) {
			for(NSXMLNode* imagenode in [images subarrayWithRange:NSMakeRange(lastindex,[images count]-lastindex)]) {
				NSString* image = [imagenode stringValue];
				NSString* fullpath = [output stringByAppendingPathComponent:[image lastPathComponent]];
				if([[NSFileManager defaultManager] fileExistsAtPath:fullpath])
					NSLog(@"%@ exists, skipping.",fullpath);
				else if([[NSData dataWithContentsOfURL:[NSURL URLWithString:image]] writeToFile:fullpath atomically:NO]) {
					NSLog(@"%@ saved to %@",image,fullpath);
					downtot++;
				}
				else NSLog(@"Error writing %@",fullpath);
			}
			lastindex = [images count];
		}
		NSTimeInterval looptime = [[NSDate date] timeIntervalSinceDate:lasttime];
		[loop drain];
		NSLog(@"Finished in %0.2f seconds.",looptime);
		// No need for looptime at this point so may as well reuse it in the most obnoxious way possible
		if((looptime = refresh - looptime) > 0) {
			NSLog(@"Sleeping for %0.2f seconds.",looptime);
			[NSThread sleepForTimeInterval:looptime];
		}
	} while(refresh && thread);
	NSLog(@"%@: got %d images total.",url,downtot);
	
	[pool drain];
	return 0;
}