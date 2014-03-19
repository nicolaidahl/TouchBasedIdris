//
// Created by Nicolai Dahl on 05/03/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTMainViewController.h"
#import "IDTAbstractViewModel.h"
#import "IDTMainViewModel.h"
#import "IDTMainView.h"
#import "IDTContextViewController.h"


@interface IDTMainViewController ()

@property (nonatomic, readonly) IDTMainView *mainView;
@property (nonatomic, readonly) IDTMainViewModel *viewModel;

@end

@implementation IDTMainViewController {
    IDTMainViewModel *_viewModel;
    IDTMainView *_mainView;
    UIPopoverController *_contextPopoverController;
}

- (id)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil {
    self = [super initWithNibName:nibNameOrNil bundle:nibBundleOrNil];
    if (self) {
        _viewModel = [[IDTMainViewModel alloc] init];

        self.mainView.addTopLevelDecButton.rac_command = _viewModel.addTopLevelDecCommand;
        [[_viewModel.addTopLevelDecCommand.executionSignals flatten] subscribeNext:^(id x) {
            [self showContextPickerFromView: _mainView.addTopLevelDecButton];
        }];
    }

    return self;
}

- (void)showContextPickerFromView:(UIView *)view {

    IDTContextViewController *cvc = [[IDTContextViewController alloc] init];
    [[cvc.selectionCommand.executionSignals flatten] subscribeNext:^(NSNumber *index) {
        if([index isEqualToNumber:@0])
            [_mainView addDataDeclaration];
        else if([index isEqualToNumber:@1])
            [_mainView addFunctionDeclaration];

        [_contextPopoverController dismissPopoverAnimated:YES];
        _contextPopoverController = nil;

        [_mainView updateConstraints];
    }];

    _contextPopoverController = [[UIPopoverController alloc] initWithContentViewController:cvc];

    CGRect convertedRect = [view convertRect:view.bounds toView:self.mainView];
    [_contextPopoverController presentPopoverFromRect:convertedRect inView:self.mainView
                             permittedArrowDirections:UIPopoverArrowDirectionAny animated:YES];


}


- (void)viewDidLoad {



    [super viewDidLoad];


}







#pragma mark - Accessors

- (IDTMainView *)mainView {
    if(!_mainView)
    {
        _mainView = [[IDTMainView alloc] initAndLayout];
    }
    
    return _mainView;
}

- (IDTAbstractViewModel *)viewModel {
    if(!_viewModel) 
    {
        _viewModel = [IDTMainViewModel new];
    }
    
    return _viewModel;
}


@end