//
// Created by Nicolai Dahl on 14/03/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "IDTAbstractView.h"
#import "IDTAbstractHierarchyView.h"
#import "IDTAbstractTopLevelDeclarationView.h"
#import "IDTInferenceRuleView.h"

@interface IDTDataDeclarationView : IDTAbstractTopLevelDeclarationView

@property (nonatomic, strong) RACCommand *addedNewConstructorCommand;

@property (nonatomic, strong) IDTInferenceRuleView *typeDeclaration;
@end