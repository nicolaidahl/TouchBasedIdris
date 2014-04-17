//
// Created by Nicolai Dahl on 05/03/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTAbstractView.h"
#import "UIView+ClassyLayoutProperties.h"


@implementation IDTAbstractView {


}

- (id)initAndLayout
{
    self = [super initWithFrame:CGRectZero];
    if (self) {
        [self runInitialLayoutRoutine];
    }
    return self;

}

- (void) runInitialLayoutRoutine
{
    [self addSubviews];
    [UIView recursivelyUpdateStylingImmediately:self];
    [self defineLayout];
}

- (void)updateConstraints {
    [self defineLayout];
    [super updateConstraints];
}

- (void)addSubviews {
    NSAssert(NO, @"Must override");
}

- (void)defineLayout {
    NSAssert(NO, @"Must override");
}


@end