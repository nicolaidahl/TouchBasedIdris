//
// Created by Nicolai Dahl on 08/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTProgram.h"


@implementation IDTProgram {

}


- (NSMutableArray *)topLevelDecs {
    return [@[] mutableCopy];
}


- (NSDictionary *)dictionaryRepresentation {

    return @{@"name": self.name,
             @"topLevelDec": self.topLevelDecs};

}




@end