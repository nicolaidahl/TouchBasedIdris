//
// Created by Nicolai Dahl on 19/03/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTAbstractHierarchyView.h"


@implementation IDTAbstractHierarchyView {

}
- (UIView *)viewThatConnectsThisToViewHierarchy {
    NSAssert(NO, @"Must override");
    return nil;
}

@end