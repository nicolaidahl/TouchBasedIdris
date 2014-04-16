//
// Created by Nicolai Dahl on 08/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTLambda.h"


@implementation IDTLambda {

}

- (NSDictionary *)dictionaryRepresentation {
    return @{@"tag": @"TILam",
            @"contents": @[self.identifier, self.expression]};
}

@end