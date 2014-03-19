//
// Created by Nicolai Dahl on 05/03/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTAbstractViewModel.h"


@implementation IDTAbstractViewModel {

}
- (RACCommand *)selectionCommand {
    if(!_selectionCommand)
    {
        _selectionCommand = [[RACCommand alloc] initWithSignalBlock:^RACSignal *(NSNumber *index) {
            return [RACSignal return:index];
        }];
    }

    return _selectionCommand;
}

@end