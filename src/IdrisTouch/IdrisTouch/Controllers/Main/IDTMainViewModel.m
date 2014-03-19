//
// Created by Nicolai Dahl on 05/03/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTMainViewModel.h"

@interface IDTMainViewModel ()



@end

@implementation IDTMainViewModel {

}
- (RACCommand *)addTopLevelDecCommand {
    if(!_addTopLevelDecCommand)
    {
        _addTopLevelDecCommand = [[RACCommand alloc] initWithSignalBlock:^RACSignal *(id input) {
            return [RACSignal return:nil];
        }];
    }

    return _addTopLevelDecCommand;
}

@end