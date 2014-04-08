//
// Created by Nicolai Dahl on 08/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTExpression.h"


@implementation IDTExpression {

}

- (NSDictionary *)dictionaryRepresentation {
    NSAssert(NO, @"Must be overwritten in subclass");
    return nil;
}

@end