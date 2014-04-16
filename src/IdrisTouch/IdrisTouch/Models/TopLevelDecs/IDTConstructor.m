//
// Created by Nicolai Dahl on 08/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTConstructor.h"
#import "IDTExpression.h"


@implementation IDTConstructor {

}


- (NSDictionary *)dictionaryRepresentation {
    return @{@"constructor": self.constructor,
             @"constructorType": self.constructorType};

}


@end