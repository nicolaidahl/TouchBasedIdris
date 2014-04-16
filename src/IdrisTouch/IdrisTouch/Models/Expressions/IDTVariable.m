//
// Created by Nicolai Dahl on 08/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTVariable.h"


@implementation IDTVariable {

}

- (NSDictionary *)dictionaryRepresentation {
    return @{@"tag": @"TIVar",
            @"contents": self.variableName};
}

@end