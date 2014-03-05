//
//  Created by Ole Gammelgaard Poulsen on 21/01/14.
//  Copyright (c) 2014 SHAPE A/S. All rights reserved.
//

#import "IDTAbstractViewController.h"
#import "IDTAbstractViewModel.h"
#import "IDTAbstractView.h"


@implementation IDTAbstractViewController {

}

- (void)loadView {
	[super loadView];
	self.view = self.mainView;
}

- (void)viewDidLoad {
    [super viewDidLoad];
//    if([self.navigationController.viewControllers count] > 1){
//        UIButton *backButton = [UIButton buttonWithType:UIButtonTypeCustom];
//        UIImage *image = kImgBackArrow;
//        [backButton setImage:image forState:UIControlStateNormal];
//        backButton.frame = CGRectMake(0.f, 0.f, image.size.width, image.size.height);
//        [backButton addTarget:self action:@selector(pop:) forControlEvents:UIControlEventTouchUpInside];
//        UIBarButtonItem *barButtonItem = [[UIBarButtonItem alloc] initWithCustomView:backButton];
//        [self.navigationItem setLeftBarButtonItem:barButtonItem];
//    }
}

- (void)pop:(id)sender
{
    [self.navigationController popViewControllerAnimated:YES];
}

- (IDTAbstractView *)mainView {
    NSAssert(NO, @"You must override this method in a subclass");
    return nil;
}

- (IDTAbstractViewModel *)viewModel {
    NSAssert(NO, @"You must override this method in a subclass");
    return nil;
}

@end