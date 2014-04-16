//
// Created by Nicolai Dahl on 08/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTPi.h"


@implementation IDTPi {

}

- (NSDictionary *)dictionaryRepresentation {
    NSMutableArray *contentsArray = [@[self.expr1, self.expr2] mutableCopy];
    self.identifier ?: [contentsArray addObject:self.identifier];

    return @{@"tag": @"TILPi",
            @"contents": contentsArray};
}

@end