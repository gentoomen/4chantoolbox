/*
 *  4chantoolbox - Educational Yotsuba scripts and tools.
 *  Copyright 2011 gentoomen. All rights reserved.
 *	This file is distributed under the terms of the Do What The Fuck You Want To Public License, Version 2.
 *  	Or, at the discretion of the project, any comparable license chosen.
 * 
 *	Scraper-ChanKit.m - ChanKit-based image fetcher.
 */

#import <Cocoa/Cocoa.h>
#import <ChanKit/ChanKit.h>

int main (int argc, const char * argv[]) {
	NSAutoreleasePool* pool = [[NSAutoreleasePool alloc] init];
	
	NSUserDefaults* args = [NSUserDefaults standardUserDefaults];
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

	NSURL* url = [NSURL URLWithString:[args stringForKey:@"u"]];
	NSFileManager* filemanager = [NSFileManager defaultManager];
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

	CKThread* thread = [CKThread threadReferencingURL:url];
	NSUInteger lastindex = 0;
	int status;
	NSUInteger downtot = 0;
	do {
		NSAutoreleasePool* loop = [[NSAutoreleasePool alloc] init];
		NSDate* lasttime = [NSDate date];		
		status = [thread populate];
		if(status == CK_ERR_SUCCESS)
			NSLog(@"Thread #%@ successfully fetched.",thread.IDString);
		else
			NSLog(@"Failed to fetch %@, error code: %d",thread.URL,status);
		for(CKImage* image in [thread.images subarrayWithRange:NSMakeRange(lastindex,thread.imagecount-lastindex)]) {
			NSString* fullpath = [output stringByAppendingPathComponent:image.name];
			if([filemanager fileExistsAtPath:fullpath])
				NSLog(@"%@ exists, skipping.",fullpath);
			else if([filemanager createFileAtPath:fullpath contents:image.data attributes:[NSDictionary dictionaryWithObject:image.timestamp forKey:@"NSFileModificationDate"]]) {
				NSLog(@"%@ saved to %@",image.URL,fullpath);
				downtot++;
			}
			else NSLog(@"Error writing %@",fullpath);
		}
		lastindex = thread.imagecount;
		NSTimeInterval looptime = [[NSDate date] timeIntervalSinceDate:lasttime];
		[loop drain];
		NSLog(@"Finished in %0.2f seconds.",looptime);
		if((looptime = refresh - looptime) > 0) {
			NSLog(@"Sleeping for %0.2f seconds.",looptime);
			[NSThread sleepForTimeInterval:looptime];
		}
	} while(refresh && status == CK_ERR_SUCCESS);
	
	if(downtot) {
		NSDateFormatter* dateformat = [[NSDateFormatter alloc] init];
		[dateformat setDateFormat:@"MM/dd/yy(EEE)HH:mm:ss"];
		NSArray* imageposts = thread.imagePosts;
		NSLog(@"%@: got %d images total between %@ and %@.",thread.URL,downtot,	[dateformat stringFromDate:[[imageposts objectAtIndex:0] date]],
																				[dateformat stringFromDate:[[imageposts lastObject] date]]);
		[dateformat release];
	}
	else NSLog(@"%@: didn't get any images.",thread.URL);
	
	[pool drain];
	return 0;
}