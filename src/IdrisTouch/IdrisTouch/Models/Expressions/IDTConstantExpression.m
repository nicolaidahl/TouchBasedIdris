//
// Created by Nicolai Dahl on 08/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTConstantExpression.h"
#import "IDTConstant.h"


@implementation IDTConstantExpression {

}

- (id)initWithConstant: (IDTConstant*) constant {
    self = [super init];
    if (self) {
        self.constant = constant;
    }

    return self;
}


- (NSDictionary *)dictionaryRepresentation {
    return @{@"tag": @"TIConst",
             @"contents": self.constant};
}


@end