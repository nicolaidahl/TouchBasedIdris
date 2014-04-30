//
// Created by Nicolai Dahl on 19/03/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "IDTAbstractTopLevelDeclarationView.h"

@class IDTTextFieldGroupInputView;
@class IDTClauseGroupInputView;


@interface IDTFunctionDeclarationView : IDTAbstractTopLevelDeclarationView
@property (nonatomic, strong) UIButton *addLineButton;

@property (nonatomic, strong) RACCommand *addedNewClauseCommand;

@property (nonatomic, strong) IDTTextFieldGroupInputView *typeDeclaration;

@property (nonatomic, strong) NSArray *clauses;

- (void)addClauseViewWithTexts:(NSArray *)array;

@end