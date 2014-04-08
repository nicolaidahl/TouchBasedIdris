//
// Created by Nicolai Dahl on 08/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTClause.h"
#import "IDTExpression.h"


@implementation IDTClause {

}


- (NSArray *)lhs {
    if(!_lhs)
    {
        _lhs = [[[NSArray alloc] init] mutableCopy];
    }

    return _lhs;
}


- (NSDictionary *)dictionaryRepresentation {

    return @{@"lhs": self.lhs,
             @"rhs": self.rhs};

}


@end