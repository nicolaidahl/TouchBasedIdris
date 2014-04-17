//
// Created by Nicolai Dahl on 05/03/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import <Foundation/Foundation.h>


@interface IDTAbstractView : UIView

// The use of this constructor will automatically layout the view
- (id)initAndLayout;

// If initAndLayout is not used as constructor the user should call this method for initial layout
- (void) runInitialLayoutRoutine;

// You must override these methods in subclasses

/// Override this method and add all subviews in it
- (void)addSubviews;

/// Override this method and use the mas_updateConstraints... methods to add constraints
/// It's important to use the 'update' methods as this may be called multiple times.
- (void)defineLayout;


@end