//
// Created by Nicolai Dahl on 08/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTConstantFloatType.h"


@implementation IDTConstantFloatType {

}

- (NSDictionary *)dictionaryRepresentation {
    return @{@"tag": @"TIFloatTy",
            @"contents": @[]};
}

@end