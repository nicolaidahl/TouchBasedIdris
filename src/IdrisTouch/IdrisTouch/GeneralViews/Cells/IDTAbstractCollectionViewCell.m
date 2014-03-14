//
// Created by Nicolai Dahl on 07/03/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTAbstractCollectionViewCell.h"


@implementation IDTAbstractCollectionViewCell {

}

- (id)initWithFrame:(CGRect)frame {
    if (self = [super initWithFrame:frame]) {
        [self addSubviews];
        [UIView recursivelyUpdateStylingImmediately:self];
        [self defineLayout];
    }
    return self;
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