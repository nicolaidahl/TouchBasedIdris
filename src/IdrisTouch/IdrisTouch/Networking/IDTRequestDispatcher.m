//
// Created by Nicolai Dahl on 08/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTRequestDispatcher.h"
#import "CJSONDeserializer.h"

@interface IDTRequestDispatcher () <NSURLConnectionDelegate>

@property (nonatomic, strong) NSError *error;
@property (nonatomic, strong) NSData *responseData;
@property (nonatomic, assign) BOOL completed;

@end


static NSString *const endPoint = @"http://localhost:8000/";
static NSString *const IDTHTTPMethodPost = @"POST";
static NSString *const IDTHTTPMethodGet = @"GET";

static NSString *const IDTTestPath = @"test";
static NSString *const IDTTypePath = @"type";


@implementation IDTRequestDispatcher {


}

NSString * const reactiveExtensionErrorBodyKey = @"request_error_body_key";

- (RACSignal *)dispatchRequest:(NSURLRequest *)request {
    return [RACSignal createSignal:^RACDisposable *(id <RACSubscriber> subscriber) {
        NSURLConnection *connection = [[NSURLConnection alloc] initWithRequest:request delegate:self];


        [[RACObserve(self, error) ignore:nil] subscribeNext:^(NSError *error) {
            NSError *errorWithData = [self appendBody:request.HTTPBody toError:error];
            return [subscriber sendError:errorWithData];
        }];

        [RACObserve(self, responseData) subscribeNext:^(NSMutableData *data) {
            NSData *theJSONData = data;

            CJSONDeserializer *theDeserializer = [CJSONDeserializer deserializer];
            theDeserializer.nullObject = NULL;
            NSError *theError = nil;

            NSDictionary *theObject = [theDeserializer deserialize:theJSONData error:&theError];

            if(theError)
            {
                NSLog(@"%@", theError.userInfo);
                self.error = theError;
            }
            else
            {

                [subscriber sendNext:theObject];
            }



        }];

        [RACObserve(self, completed) subscribeNext:^(NSNumber *completed) {
            if([completed boolValue])
                [subscriber sendCompleted];
        }];

        return nil;
    }];
}

- (NSError *)appendBody:(id)body toError:(NSError *)error {
    NSMutableDictionary *errorDict = [error.userInfo mutableCopy];
    if (body) {
        [errorDict setObject:body forKey:reactiveExtensionErrorBodyKey];
    }
    NSError *errorWithData = [NSError errorWithDomain:error.domain code:error.code userInfo:[errorDict copy]];
    return errorWithData;
}

#pragma mark NSURLConnection Delegate Methods


- (void)connection:(NSURLConnection *)connection didReceiveResponse:(NSURLResponse *)response {
    // A response has been received, this is where we initialize the instance var you created
    // so that we can append data to it in the didReceiveData method
    // Furthermore, this method is called each time there is a redirect so reinitializing it
    // also serves to clear it
    self.responseData = [[NSMutableData alloc] init];
}

- (void)connection:(NSURLConnection *)connection didReceiveData:(NSData *)data {
    // Append the new data to the instance variable you declared
    self.responseData = data;
}

- (NSCachedURLResponse *)connection:(NSURLConnection *)connection
                  willCacheResponse:(NSCachedURLResponse*)cachedResponse {
    // Return nil to indicate not necessary to store a cached response for this connection 
    return nil;
}

- (void)connectionDidFinishLoading:(NSURLConnection *)connection {
    // The request is complete and data has been received
    // You can parse the stuff in your instance variable now

}

- (void)connection:(NSURLConnection *)connection didFailWithError:(NSError *)error {
    self.error = error;
}




#pragma mark - JSON request builder

- (NSMutableURLRequest *) standardJSONURLGetRequest
{
    NSURL *url = [NSURL URLWithString:[NSString stringWithFormat:@"%@%@", endPoint, IDTTestPath]];

    // Create the request.
    NSMutableURLRequest *request = [NSMutableURLRequest requestWithURL:url];

    request.HTTPMethod = IDTHTTPMethodGet;

    // This is how we set header fields
    [request setValue:@"application/json; charset=utf-8" forHTTPHeaderField:@"Content-Type"];

    return request;
}

- (NSMutableURLRequest *) standardJSONURLPostRequest
{
    NSURL *url = [NSURL URLWithString:[NSString stringWithFormat:@"%@%@", endPoint, IDTTypePath]];

    // Create the request.
    NSMutableURLRequest *request = [NSMutableURLRequest requestWithURL:url];

    request.HTTPMethod = IDTHTTPMethodPost;

    // This is how we set header fields
    [request setValue:@"application/json; charset=utf-8" forHTTPHeaderField:@"Content-Type"];

    return request;
}


@end