//
//  Created by Ole Gammelgaard Poulsen on 21/01/14.
//  Copyright (c) 2014 SHAPE A/S. All rights reserved.
//

#import <Foundation/Foundation.h>

@class IDTAbstractViewModel;
@class IDTAbstractView;


@interface IDTAbstractViewController : UIViewController

@property (nonatomic, readonly) IDTAbstractView *mainView;
@property (nonatomic, readonly) IDTAbstractViewModel *viewModel;


@end