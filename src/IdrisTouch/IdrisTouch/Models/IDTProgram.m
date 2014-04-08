//
// Created by Nicolai Dahl on 08/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTProgram.h"


@implementation IDTProgram {

}

- (id)initWithName: (NSString *) name {
    self = [super init];
    if (self) {
        self.name = name;
    }

    return self;
}


- (NSMutableArray *)topLevelDecs {
    return [@[] mutableCopy];
}


- (NSDictionary *)dictionaryRepresentation {

    return @{@"name": self.name,
             @"topLevelDec": self.topLevelDecs};

}




@end