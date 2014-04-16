//
// Created by Nicolai Dahl on 08/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTConstantInt.h"


@implementation IDTConstantInt {

}

- (id)initWithInt: (NSNumber *) integer {
    self = [super init];
    if (self) {
        self.integer = integer;
    }

    return self;
}


- (NSDictionary *)dictionaryRepresentation {
    return @{@"tag": @"TIInt",
            @"contents": self.integer};
}

@end