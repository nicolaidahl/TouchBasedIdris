//
// Created by Nicolai Dahl on 19/03/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTFunctionDeclarationView.h"
#import "IDTTextFieldGroupInputView.h"
#import "IDTClauseGroupInputView.h"
#import "IDTMetaVariableInputView.h"
#import "IDTClause.h"

@interface IDTFunctionDeclarationView ()

@property (nonatomic, strong) UIView *connectingLine;
@property (nonatomic, strong) UILabel *functionNameLabel;
@property (nonatomic, strong) UIView *verticalLine;
@property (nonatomic, strong) NSMutableArray *lineActionLineTuples;

@end

@implementation IDTFunctionDeclarationView {

    NSArray *_constructorConstraints;
}

- (id)initAndLayout {
    self = [super initAndLayout];
    if (self) {
        [[self.addLineButton rac_signalForControlEvents:UIControlEventTouchUpInside] subscribeNext:^(id x) {


            [self addClauseViewWithTexts:@[@"xs", @"ys"]];
            [self updateConstraints];
        }];
    }

    return self;
}

- (void)addSubviews {
    [self addSubview:self.connectingLine];
    [self addSubview:self.functionNameLabel];
    [self addSubview:self.typeDeclaration];
    [self addSubview:self.verticalLine];
    [self addSubview:self.addLineButton];
}

- (void)defineLayout {
    [self.connectingLine mas_updateConstraintsWithLeftMarginRelativeToSuperview];
    [self.connectingLine mas_updateConstraintsWidthFromStylesheet];
    [self.connectingLine mas_updateConstraintsHeightFromStylesheet];
    [self.connectingLine mas_updateConstraints:^(MASConstraintMaker *make) {
        make.centerY.equalTo(self.functionNameLabel);
    }];

    [self.functionNameLabel mas_updateConstraintsWithLeftMarginRelativeTo:self.connectingLine.mas_right];
    [self.functionNameLabel mas_updateConstraintsWithRightMarginRelativeTo:self.typeDeclaration.mas_left];
    [self.functionNameLabel mas_updateConstraints:^(MASConstraintMaker *make) {
        make.centerY.equalTo(self.typeDeclaration);
    }];
    [self.functionNameLabel mas_updateConstraintsHeightFromStylesheet];

    [self.verticalLine mas_updateConstraintsWidthFromStylesheet];
    [self.verticalLine mas_updateConstraintsWithTopMarginRelativeTo:self.functionNameLabel.mas_bottom];
    [self.verticalLine mas_updateConstraints:^(MASConstraintMaker *make) {
        make.centerX.equalTo(self.functionNameLabel);
    }];

    [self.addLineButton mas_updateConstraints:^(MASConstraintMaker *make) {
        make.centerX.equalTo(self.verticalLine);
        make.top.equalTo(self.verticalLine.mas_bottom);
    }];
    [self.addLineButton mas_updateConstraintsWithBottomMarginRelativeToSuperview];


    [self.typeDeclaration mas_updateConstraintsWithTopMarginRelativeToSuperview];
    [self.typeDeclaration mas_updateConstraints:^(MASConstraintMaker *make) {
        make.right.lessThanOrEqualTo(self.typeDeclaration.superview);
    }];

    [self.lineActionLineTuples enumerateObjectsUsingBlock:^(RACTuple *tuple, NSUInteger idx, BOOL *stop) {

        UIView *functionLine = tuple.second;


        UIView *topNeighbor;
        if(idx == 0)
        {
            topNeighbor = self.typeDeclaration;
        }
        else
        {
            topNeighbor = ((RACTuple *)_lineActionLineTuples[idx-1]).second;
        }

        [functionLine mas_updateConstraints:^(MASConstraintMaker *make) {
            make.top.equalTo(topNeighbor.mas_bottom).with.offset(10);
            make.left.equalTo(self.verticalLine.mas_right).with.offset(6);
            make.right.lessThanOrEqualTo(functionLine.superview);
        }];
    }];

    [_constructorConstraints enumerateObjectsUsingBlock:^(MASConstraint *constraint, NSUInteger idx, BOOL *stop) {
        [constraint uninstall];
    }];

    if(self.lineActionLineTuples.count == 0)
    {
        _constructorConstraints = [self.addLineButton mas_updateConstraints:^(MASConstraintMaker *make) {
            make.top.equalTo(self.typeDeclaration.mas_bottom);
        }];
    }
    else
    {
        UIView *lowestLine = ((RACTuple*)self.lineActionLineTuples[_lineActionLineTuples.count - 1]).second;

        _constructorConstraints = [self.addLineButton mas_updateConstraints:^(MASConstraintMaker *make) {
            make.top.equalTo(lowestLine.mas_bottom);
        }];
    }

}

- (NSArray *)clauses {


    return [self.lineActionLineTuples.rac_sequence map:^id(RACTuple *tuple) {
        return tuple.second;
    }].array;
}

- (void) addClauseViewWithTexts: (NSArray*) array
{

    IDTClauseGroupInputView *clauseGroupInputView = [self addClauseView];

    int counter = 0;
    for (IDTTextFieldInputView *fieldInputView in clauseGroupInputView.lhs.inputViews) {
        if(array.count > counter)
            fieldInputView.textField.text = array[(NSUInteger) counter];

        counter ++;
    }
}

- (IDTClauseGroupInputView *) addClauseView {

    RACSequence *inputViewsWithContent = [self.typeDeclaration.inputViews.rac_sequence filter:^BOOL
    (IDTTextFieldInputView *inputView) {
        return ![inputView.textField.text isEqualToString:@""];
    }];

    IDTTextFieldGroupInputView *lhs = [[IDTTextFieldGroupInputView alloc]
            initAndLayoutWithExactNumberOfInputViews:@(inputViewsWithContent.array.count - 1)
                                       separatorType:IDTGroupInputViewSeparatorSmallSpace
                                       andBoderStyle:IDTInputBorderStyleSolid];


    IDTMetaVariableInputView *rhs = [[IDTMetaVariableInputView alloc] initAndLayout];

    IDTClauseGroupInputView *clauseGroupInputView = [[IDTClauseGroupInputView alloc]
            initAndLayoutWithLhsInputView:lhs andRhsInputView:rhs];
    clauseGroupInputView.cas_styleClass = @"data-dec-constructor";


    UIButton *lineActionButton = [UIButton buttonWithType:UIButtonTypeCustom];
    [lineActionButton setImage:[UIImage imageNamed:@"line_action_button"] forState:UIControlStateNormal];

    [self addSubview:clauseGroupInputView];
    [self.lineActionLineTuples addObject:RACTuplePack(lineActionButton, clauseGroupInputView)];

    [self.addedNewClauseCommand execute:clauseGroupInputView];

    return clauseGroupInputView;
}


- (UIView *)viewThatConnectsThisToViewHierarchy {
    return self.connectingLine;
}


#pragma mark - Accessors

- (UIView *)connectingLine {
    if(!_connectingLine)
    {
        _connectingLine = [[UIView alloc] init];
        _connectingLine.cas_styleClass = @"function-dec-connecting-line";
    }

    return _connectingLine;
}

- (UILabel *)functionNameLabel {
    if(!_functionNameLabel)
    {
        _functionNameLabel = [UILabel new];
        _functionNameLabel.cas_styleClass = @"function-name-label";
        _functionNameLabel.text = @"zip";
    }

    return _functionNameLabel;
}

- (IDTTextFieldGroupInputView *)typeDeclaration {
    if(!_typeDeclaration)
    {
        _typeDeclaration = [[IDTTextFieldGroupInputView alloc] initAndLayoutWithExactNumberOfInputViews:nil
                                                                                 separatorType:IDTGroupInputViewSeparatorArrow
                                                                                 andBoderStyle:IDTInputBorderStyleSolid];
        _typeDeclaration.cas_styleClass = @"function-dec-type-dec";

    }

    return _typeDeclaration;
}


- (UIView *)verticalLine {
    if(!_verticalLine)
    {
        _verticalLine = [[UIView alloc] init];
        _verticalLine.cas_styleClass = @"function-dec-vertical-line";
    }
    return _verticalLine;
}


- (NSMutableArray *)lineActionLineTuples {
    if(!_lineActionLineTuples)
    {
        _lineActionLineTuples = [@[] mutableCopy];
    }
    return _lineActionLineTuples;
}

- (UIButton *)addLineButton {
    if(!_addLineButton)
    {
        _addLineButton = [UIButton buttonWithType:UIButtonTypeCustom];
        [_addLineButton setImage:[UIImage imageNamed:@"add_button"] forState:UIControlStateNormal];
        _addLineButton.cas_styleClass = @"function-dec-add-button";
    }

    return _addLineButton;
}


- (RACCommand *)addedNewClauseCommand {
    if(!_addedNewClauseCommand)
    {
        _addedNewClauseCommand = [[RACCommand alloc] initWithSignalBlock:^RACSignal *(IDTClauseGroupInputView *clause) {
            return [RACSignal return:clause];
        }];

    }

    return _addedNewClauseCommand;
}


@end