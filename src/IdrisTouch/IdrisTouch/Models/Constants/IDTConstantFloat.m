//
// Created by Nicolai Dahl on 08/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTConstantFloat.h"


@implementation IDTConstantFloat {

}

- (id)initWithFloat: (NSNumber *)floatingPoint{
    self = [super init];
    if (self) {
        self.floatingPoint = floatingPoint;
    }

    return self;
}


- (NSDictionary *)dictionaryRepresentation {
    return @{@"tag": @"TIFloat",
            @"contents": self.floatingPoint};
}


@end