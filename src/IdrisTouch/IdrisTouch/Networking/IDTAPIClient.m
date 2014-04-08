//
// Created by Nicolai Dahl on 08/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTAPIClient.h"
#import "IDTRequestDispatcher.h"
#import "IDTProgram.h"
#import "IDTJSONSerializer.h"


@implementation IDTAPIClient {

}




- (RACSignal *) getEvaluationOfObjectHierarchy: (IDTProgram* ) program
{
    IDTRequestDispatcher *requestDispatcher = [[IDTRequestDispatcher alloc] init];
    NSMutableURLRequest *request = [requestDispatcher standardJSONURLGetRequest];

    NSData *jsonData = [[IDTJSONSerializer serializer] serializeObjectHierarchyToData:program];

    NSData *requestBodyData = jsonData;
    request.HTTPBody = requestBodyData;

    return [requestDispatcher dispatchRequest:request];
}

@end