//
// Created by Nicolai Dahl on 08/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTTopLevelDataDec.h"


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

    return @{@"tag": @"TIDataDec",
             @"constructors": self.constructors};

}




@end