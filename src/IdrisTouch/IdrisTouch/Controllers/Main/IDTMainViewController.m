//
// Created by Nicolai Dahl on 05/03/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTMainViewController.h"
#import "IDTAbstractViewModel.h"
#import "IDTMainViewModel.h"
#import "IDTMainView.h"


@interface IDTMainViewController ()

@property (nonatomic, strong) IDTMainViewModel *viewModel;

@end

@implementation IDTMainViewController {
    IDTMainViewModel *_viewModel;
    IDTMainView *_mainView;
}

- (IDTMainView *)mainView {
    if(!_mainView)
    {
        _mainView = [[IDTMainView alloc] initAndLayout];
    }
    
    return nil;
}

- (IDTAbstractViewModel *)viewModel {
    if(!_viewModel) 
    {
        _viewModel = [IDTMainViewModel new];
    }
    
    return _viewModel;
}


@end