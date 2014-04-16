//
// Created by Nicolai Dahl on 08/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTTopLevelFuncDec.h"
#import "IDTExpression.h"


@implementation IDTTopLevelFuncDec {

}

- (id)init {
    self = [super init];
    if (self) {
        self.clauses = [@[] mutableCopy];
    }

    return self;
}


- (NSDictionary *)dictionaryRepresentation {
    NSMutableDictionary *mutableDictionary = [@{@"tag": @"TIFuncDec",
                                                @"ident": self.ident,
                                                @"clauses": self.clauses} mutableCopy];
    !self.titype ?: [mutableDictionary setObject:self.titype forKey:@"titype"];

    return mutableDictionary;

}


@end