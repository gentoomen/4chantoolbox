/*
 *  4chantoolbox - Educational Yotsuba scripts and tools.
 *  Copyright 2011 gentoomen. All rights reserved.
 *	This file is distributed under the terms of the Do What The Fuck You Want To Public License, Version 2. 
 *  	Or, at the discretion of the project, any comparable license chosen.
 * 
 *	Scraper-ObjC.m - Canonical FoundationKit image fetcher.
 */

#import <Cocoa/Cocoa.h>

int main (int argc, const char * argv[]) {
	NSAutoreleasePool* pool = [[NSAutoreleasePool alloc] init];
		
	NSUserDefaults* args = [NSUserDefaults standardUserDefaults];
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
	
	NSURL* url = [NSURL URLWithString:[args stringForKey:@"u"]];
	NSString* output;
	if([args objectForKey:@"o"])
		output = [[args stringForKey:@"o"] stringByStandardizingPath];
	else 
		output = [[NSFileManager defaultManager] currentDirectoryPath];
	NSInteger refresh = 10;
	if([args objectForKey:@"t"])
		refresh = [args integerForKey:@"t"];

	NSLog(@"Downloading %@",url);
	NSLog(@"Saving to location %@",output);
	if(refresh)	NSLog(@"Timer set to every %d seconds.",refresh);

	NSXMLDocument* thread;
	NSUInteger lastindex = 0;
	NSError* e = nil;
	NSUInteger downtot = 0;
	do {
		NSAutoreleasePool* loop = [[NSAutoreleasePool alloc] init];
		NSDate* lasttime = [NSDate date];
		thread = [[NSXMLDocument alloc] initWithContentsOfURL:url options:NSXMLDocumentTidyHTML error:&e];
		if(thread)
			NSLog(@"%@ successfully fetched.",url);
		else
			NSLog(@"Failed to fetch %@, error: %@",url,[e localizedDescription]);
		NSArray* images = [thread nodesForXPath:@"/html/body/form//a[img[@md5]]/@href" error:NULL];
		[thread release];
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
		if((looptime = refresh - looptime) > 0) {
			NSLog(@"Sleeping for %0.2f seconds.",looptime);
			[NSThread sleepForTimeInterval:looptime];
		}
	} while(refresh && thread);
	NSLog(@"%@: got %d images total.",url,downtot);
	
	[pool drain];
	return 0;
}