//
// Created by Nicolai Dahl on 08/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTConstantStringType.h"


@implementation IDTConstantStringType {

}

- (NSDictionary *)dictionaryRepresentation {
    return @{@"tag": @"TIStringTy",
            @"contents": @[]};
}

@end