//
// Created by Nicolai Dahl on 19/03/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "IDTAbstractView.h"

typedef NS_ENUM(NSInteger, IDTHierarchyMoveDirection) {
    IDTHierarchyMoveDirectionUp,
    IDTHierarchyMoveDirectionDown
};

@interface IDTAbstractHierarchyView : IDTAbstractView


//Must be overwritten to show which view "connects" this view to the higher level
- (UIView*)viewThatConnectsThisToViewHierarchy;

@end