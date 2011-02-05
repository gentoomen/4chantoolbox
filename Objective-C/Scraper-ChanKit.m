/*
 *  4chantoolbox - Educational Yotsuba scripts and tools.
 *  Copyright 2011 gentoomen. All rights reserved.
 *  This file is distributed under the terms of the Do What The Fuck You Want To Public License, Version 2.
 *  	Or, at the discretion of the project, any comparable license chosen.
 * 
 *  Scraper-ChanKit.m - ChanKit-based image fetcher.
 */

#import <Cocoa/Cocoa.h>
#import <ChanKit/ChanKit.h>

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
			  	"ChanKit v%@\n"
	    		"\n"
				"OPTIONS\n"
	    		" -u <url>\t url to download\n"
	    		" -o <path>\t output dir [./]\n"
	    		" -t <int>\t thread refresh in seconds [10]\n"
	    		"\n",[CKUtil version]);
		[pool drain];
		return 0;
	}	

	// Now we can just ask the userdefaults for a string associated with flag "u"
	NSURL* url = [NSURL URLWithString:[args stringForKey:@"u"]];
	NSFileManager* filemanager = [NSFileManager defaultManager];
	NSString* output;
	if([args objectForKey:@"o"])
		// stringByStandardizingPath is a great method that sanitizes a path by expanding tildes, resolving symlinks, compacting extraneous ./, etc
		output = [[args stringForKey:@"o"] stringByStandardizingPath];
	else 
		// No directory provided, default is cwd
		output = [filemanager currentDirectoryPath];	
	NSInteger refresh = 10;
	if([args objectForKey:@"t"])
		refresh = [args integerForKey:@"t"];

	NSLog(@"Downloading %@",url);
	NSLog(@"Saving to location %@",output);
	if(refresh)	NSLog(@"Timer set to every %d seconds.",refresh);

	// This ChanKit method will prepare a thread for downloading from a given URL, but not actually hit the network until populate is called. We rely on this separation to simplify the loop.
	CKThread* thread = [CKThread threadReferencingURL:url];
	if(thread)	NSLog(@"Fetching thread #%d.",thread.ID);
	NSUInteger lastindex = 0; // Don't bother with images that have already been processed
	int status;	// This will keep track of the thread's status; if it 404s, we go offline, or any other error occurs it will abort the loop
	NSUInteger downtot = 0;
	do {
		// It's a good idea to create an inner autorelease pool for loops where many objects may be allocated; this keeps our memory footprint as small as possible
		NSAutoreleasePool* loop = [[NSAutoreleasePool alloc] init];
		NSDate* lasttime = [NSDate date];		
		// Download and parse the thread
		status = [thread populate];
		// Sure would be nice to have a subarrayFromIndex: method. If we weren't watching the thread we could simply use:
		//		for(CKImage* image in thread.images)
		for(CKImage* image in [thread.images subarrayWithRange:NSMakeRange(lastindex,thread.imagecount-lastindex)]) {
			NSString* fullpath = [output stringByAppendingPathComponent:image.name];
			int imagestatus;
			if([filemanager fileExistsAtPath:fullpath])
				NSLog(@"%@ exists, skipping.",fullpath);
			// Check to make sure the image is reachable i.e. hasn't been deleted, we haven't gone offline, or thread didn't 404 since line 65
			else if((imagestatus = [image load]) == CK_ERR_SUCCESS) {
				// We use some of the extra metadata provided by Yotsuba to write the original filename as well as tag the modification date as the time it was uploaded to the server
				if([filemanager createFileAtPath:fullpath contents:image.data attributes:[NSDictionary dictionaryWithObject:image.timestamp forKey:@"NSFileModificationDate"]]) {
					NSLog(@"%@ saved to %@",image.URL,fullpath);
					downtot++;
				}
				else NSLog(@"Error writing %@",fullpath);
			}
			else NSLog(@"Failed to fetch %@, error: %@",image.URL,[CKUtil describeError:imagestatus]);
		}
		lastindex = thread.imagecount;
		NSTimeInterval looptime = [[NSDate date] timeIntervalSinceDate:lasttime];
		[loop drain];
		NSLog(@"Finished in %0.2f seconds.",looptime);
		// No need for looptime at this point so may as well reuse it in the most obnoxious way possible
		if((looptime = refresh - looptime) > 0) {
			NSLog(@"Sleeping for %0.2f seconds.",looptime);
			[NSThread sleepForTimeInterval:looptime];
		}
	} while(refresh && status == CK_ERR_SUCCESS);
	NSLog(@"%@: Scrape aborted: %@",thread.URL,[CKUtil describeError:status]);
	
	if(downtot) {
		// Welcome to date handling in Cocoa
		NSDateFormatter* dateformat = [[NSDateFormatter alloc] init];
		[dateformat setDateFormat:@"MM/dd/yy(EEE)HH:mm:ss"];
		NSArray* imageposts = thread.imagePosts;
		// Just because we can
		NSLog(@"%@: got %d images total between %@ and %@.",thread.URL,downtot,	[dateformat stringFromDate:[[imageposts objectAtIndex:0] date]],
																				[dateformat stringFromDate:[[imageposts lastObject] date]]);
		[dateformat release];
	}
	else NSLog(@"%@: didn't get any images.",thread.URL);
	
	[pool drain];
	return 0;
}