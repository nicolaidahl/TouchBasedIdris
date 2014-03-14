//
// Created by Nicolai Dahl on 07/03/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import <Foundation/Foundation.h>


@interface IDTAbstractCollectionViewCell : UICollectionViewCell

// You must override these methods in subclasses

// Override this method and add all subviews to the contentView in it
- (void)addSubviews;

// Override this method and use the mas_updateConstraints... methods to add constraints
// It's important to use the 'update' methods as this may be called multiple times.
- (void)defineLayout;


@end