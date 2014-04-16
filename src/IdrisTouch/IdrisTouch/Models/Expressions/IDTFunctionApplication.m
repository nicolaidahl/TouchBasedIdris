//
// Created by Nicolai Dahl on 08/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTFunctionApplication.h"


@implementation IDTFunctionApplication {

}
- (NSDictionary *)dictionaryRepresentation {
    return @{@"tag": @"TIApp",
             @"contents": @[self.function, self.arguments]};
}


@end