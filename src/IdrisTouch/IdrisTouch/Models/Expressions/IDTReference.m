//
// Created by Nicolai Dahl on 08/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTReference.h"


@implementation IDTReference {

}

- (id)initWithVarName: (NSString *) varName {
    self = [super init];
    if (self) {
        self.variableName = varName;
    }

    return self;
}


- (NSDictionary *)dictionaryRepresentation {
    return @{@"tag": @"TIRef",
            @"contents": self.variableName};
}

@end