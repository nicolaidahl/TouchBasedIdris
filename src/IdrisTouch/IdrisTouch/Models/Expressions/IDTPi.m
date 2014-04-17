//
// Created by Nicolai Dahl on 08/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTPi.h"


@implementation IDTPi {

}

- (id)initWithExpr1: (IDTExpression *)expr1 andExpr2: (IDTExpression*) expr2{
    self = [super init];
    if (self) {
        self.expr1 = expr1;
        self.expr2 = expr2;
    }

    return self;
}


- (NSDictionary *)dictionaryRepresentation {
    NSMutableArray *contentsArray = [@[self.expr1, self.expr2] mutableCopy];
    self.identifier ?: [contentsArray addObject:self.identifier];

    return @{@"tag": @"TILPi",
            @"contents": contentsArray};
}

@end