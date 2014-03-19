//
// Created by Nicolai Dahl on 05/03/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTMainView.h"
#import "IDTInferenceRuleView.h"
#import "IDTDataDeclarationView.h"
#import "IDTFunctionDeclarationView.h"

@interface IDTMainView () <UIToolbarDelegate>

@property (nonatomic, strong) UIToolbar *toolbar;
@property (nonatomic, strong) UIScrollView *scrollView;

@property (nonatomic, strong) NSMutableArray *topLevelDeclarationTuples;
@property (nonatomic, strong) UIView *verticalLine;


@end

@implementation IDTMainView {

    NSArray *_topLevelDecConstraints;
}


- (void)addSubviews {
    [self addSubview:self.toolbar];
    [self addSubview:self.scrollView];
    [self.scrollView addSubview:self.verticalLine];
    [self.scrollView addSubview:self.addTopLevelDecButton];

}

- (void) addDataDeclaration
{
    IDTDataDeclarationView *dataDeclarationView = [[IDTDataDeclarationView alloc] initAndLayout];
    dataDeclarationView.cas_styleClass = @"top-level-dec-data";

    UIButton *lineActionButton = [self newLineActionButton];
    [lineActionButton setImage:[UIImage imageNamed:@"line_action_button"] forState:UIControlStateNormal];
    [self.scrollView addSubview:lineActionButton];

    RACTuple *dataDeclarationTuple = RACTuplePack(lineActionButton, dataDeclarationView);

    [self.scrollView insertSubview:dataDeclarationView belowSubview:self.verticalLine];
    [self.topLevelDeclarationTuples addObject:dataDeclarationTuple];
}

- (void) addFunctionDeclaration
{
    IDTFunctionDeclarationView *funcDeclarationView = [[IDTFunctionDeclarationView alloc] initAndLayout];
    funcDeclarationView.cas_styleClass = @"top-level-dec-function";

    UIButton *lineActionButton = [self newLineActionButton];
    [self.scrollView addSubview:lineActionButton];

    RACTuple *funcDeclarationTuple = RACTuplePack(lineActionButton, funcDeclarationView);

    [self.scrollView insertSubview:funcDeclarationView belowSubview:self.verticalLine];
    [self.topLevelDeclarationTuples addObject:funcDeclarationTuple];
}

- (UIButton*) newLineActionButton
{
    UIButton *lineActionButton = [UIButton buttonWithType:UIButtonTypeCustom];
    [lineActionButton setImage:[UIImage imageNamed:@"line_action_button"] forState:UIControlStateNormal];
    return lineActionButton;
}

- (void)defineLayout {

    [self.toolbar mas_updateConstraintsWithLeftMarginRelativeToSuperview];
    [self.toolbar mas_updateConstraintsWithRightMarginRelativeToSuperview];
    [self.toolbar mas_updateConstraintsWithTopMarginRelativeToSuperview];

    [self.scrollView mas_updateConstraintsWithTopMarginRelativeTo:self.toolbar.mas_bottom];
    [self.scrollView mas_updateConstraintsWithLeftMarginRelativeToSuperview];
    [self.scrollView mas_updateConstraintsWithRightMarginRelativeToSuperview];
    [self.scrollView mas_updateConstraintsWithBottomMarginRelativeToSuperview];

    [self.verticalLine mas_updateConstraintsWithLeftMarginRelativeToSuperview];
    [self.verticalLine mas_updateConstraintsWithTopMarginRelativeToSuperview];
    [self.verticalLine mas_updateConstraintsWidthFromStylesheet];

    [self.addTopLevelDecButton mas_updateConstraints:^(MASConstraintMaker *make) {
        make.top.equalTo(self.verticalLine.mas_bottom);
        make.centerX.equalTo(self.verticalLine);
    }];
    [self.addTopLevelDecButton mas_updateConstraintsWithBottomMarginRelativeToSuperview];

    [self.topLevelDeclarationTuples enumerateObjectsUsingBlock:^(RACTuple *tuple, NSUInteger idx, BOOL *stop) {

        RACTupleUnpack(UIButton *lineActionButton, IDTAbstractTopLevelDeclarationView *topLevelDec) = tuple;

        UIView *viewToConnectTo = [topLevelDec viewThatConnectsThisToViewHierarchy];
        [lineActionButton mas_updateConstraints:^(MASConstraintMaker *make) {
            make.centerX.equalTo(self.verticalLine);
            make.centerY.equalTo(viewToConnectTo);
        }];

        if (idx == 0) {

            [topLevelDec mas_updateConstraints:^(MASConstraintMaker *make) {
                make.top.equalTo(self.verticalLine).with.offset(10);
            }];
        }
        else {
            UIView *topNeighbor = ((RACTuple*) _topLevelDeclarationTuples[idx - 1]).second;
            [topLevelDec mas_updateConstraints:^(MASConstraintMaker *make) {
                make.top.equalTo(topNeighbor.mas_bottom).with.offset(10);
            }];
        }

        [topLevelDec mas_updateConstraintsWithLeftMarginRelativeTo:self.verticalLine.mas_right];
        [topLevelDec mas_updateConstraints:^(MASConstraintMaker *make) {
            make.right.lessThanOrEqualTo(topLevelDec.superview);
        }];
    }];


    [_topLevelDecConstraints enumerateObjectsUsingBlock:^(MASConstraint *constraint, NSUInteger idx, BOOL *stop) {
        [constraint uninstall];
    }];

    //If there is more than one top level declaration, attach the top of the add button to the bottom of the lowest
    // data declaration. Remember the constraints so that they can be removed when the number of data decs change
    if(self.topLevelDeclarationTuples.count != 0)
    {
        NSMutableArray *constraints = [@[] mutableCopy];

        IDTAbstractTopLevelDeclarationView *lowestConstructor = ((RACTuple*) self
                        .topLevelDeclarationTuples[_topLevelDeclarationTuples.count - 1]).second;
        [constraints addObjectsFromArray:[self.addTopLevelDecButton mas_updateConstraints:^(MASConstraintMaker *make) {
            make.top.equalTo(lowestConstructor.mas_bottom);
        }]];

        _topLevelDecConstraints = constraints;
    }

}



#pragma mark - UIToolbarDelegate

- (UIBarPosition)positionForBar:(id <UIBarPositioning>)bar {
    return UIBarPositionTopAttached;
}


#pragma mark - Accessors

- (UIView *)verticalLine {
    if(!_verticalLine)
    {
        _verticalLine = [[UIView alloc] init];
        _verticalLine.cas_styleClass = @"top-level-dec-vertical-line";
    }
    return _verticalLine;
}

- (UIButton *)addTopLevelDecButton {
    if(!_addTopLevelDecButton)
    {
        _addTopLevelDecButton = [UIButton buttonWithType:UIButtonTypeCustom];
        [_addTopLevelDecButton setImage:[UIImage imageNamed:@"add_button"] forState:UIControlStateNormal];
        _addTopLevelDecButton.cas_styleClass = @"top-level-dec-add-button";
    }

    return _addTopLevelDecButton;
}

- (UIToolbar *)toolbar {
    if(!_toolbar)
    {
        _toolbar = [[UIToolbar alloc] init];
        _toolbar.delegate = self;
        _toolbar.cas_styleClass = @"main-view-toolbar";
    }

    return _toolbar;
}

- (UIScrollView *)scrollView {
    if(!_scrollView)
    {
        _scrollView = [[UIScrollView alloc] init];
        _scrollView.cas_styleClass = @"main-view-scroll-view";
    }

    return _scrollView;
}


- (NSMutableArray *)topLevelDeclarationTuples {
    if(!_topLevelDeclarationTuples)
    {
        _topLevelDeclarationTuples = [@[] mutableCopy];
    }
    return _topLevelDeclarationTuples;
}


@end