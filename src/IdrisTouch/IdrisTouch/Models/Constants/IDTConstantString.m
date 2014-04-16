//
// Created by Nicolai Dahl on 08/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTConstantString.h"


@implementation IDTConstantString {

}

- (NSDictionary *)dictionaryRepresentation {
    return @{@"tag": @"TIString",
            @"contents": self.string};
}

@end