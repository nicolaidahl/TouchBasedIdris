//
// Created by Nicolai Dahl on 08/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTTopLevelDataDec.h"
#import "IDTExpression.h"


@implementation IDTTopLevelDataDec {

}

- (NSMutableArray *)constructors {
    if(!_constructors)
    {
        _constructors = [@[] mutableCopy];
    }

    return _constructors;
}


- (NSDictionary *)dictionaryRepresentation {

    NSMutableDictionary *mutableDictionary = [@{@"tag": @"TIDataDec",
                                                @"ident": self.ident,
                                                @"constructors": self.constructors} mutableCopy];
    !self.titype ?: [mutableDictionary setObject:self.titype forKey:@"titype"];

    return mutableDictionary;

}




@end