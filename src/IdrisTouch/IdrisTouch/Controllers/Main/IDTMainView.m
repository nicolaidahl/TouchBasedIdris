//
// Created by Nicolai Dahl on 05/03/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTMainView.h"
#import "IDTInferenceRuleView.h"
#import "IDTDataDeclarationView.h"

@interface IDTMainView ()

@property (nonatomic, strong) NSMutableArray *dataDeclarationViews;
@property (nonatomic, strong) UIView *verticalLine;
@property (nonatomic, strong) UIButton *addTopLevelDecButton;

@end

@implementation IDTMainView {

    NSArray *_topLevelDecConstraints;
}

- (id)initAndLayout {
    self = [super initAndLayout];
    if (self) {
        [[self.addTopLevelDecButton rac_signalForControlEvents:UIControlEventTouchUpInside] subscribeNext:^(id x) {
            [self addDataDeclaration];
            [self updateConstraints];

        }];
    }

    return self;
}


- (void)addSubviews {
    [self addSubview:self.verticalLine];
    [self addSubview:self.addTopLevelDecButton];

}

- (void)defineLayout {

    [self.verticalLine mas_updateConstraintsWithLeftMarginRelativeToSuperview];
    [self.verticalLine mas_updateConstraintsWithTopMarginRelativeToSuperview];
    [self.verticalLine mas_updateConstraintsWidthFromStylesheet];

    [self.addTopLevelDecButton mas_updateConstraints:^(MASConstraintMaker *make) {
        make.top.equalTo(self.verticalLine.mas_bottom);
        make.centerX.equalTo(self.verticalLine);
    }];
    [self.addTopLevelDecButton mas_updateConstraintsWidthFromStylesheet];
    [self.addTopLevelDecButton mas_updateConstraintsHeightFromStylesheet];

    [self.dataDeclarationViews enumerateObjectsUsingBlock:^(UIView *topLevelDec, NSUInteger idx, BOOL *stop) {

        if(idx == 0)
        {
            
            [topLevelDec mas_updateConstraints:^(MASConstraintMaker *make) {
                make.top.equalTo(self.verticalLine).with.offset(10);
            }];
        }
        else
        {
            UIView *topNeighbor = _dataDeclarationViews[idx-1];
            [topLevelDec mas_updateConstraints:^(MASConstraintMaker *make) {
                make.top.equalTo(topNeighbor.mas_bottom).with.offset(10);
            }];
        }

        [topLevelDec mas_updateConstraints:^(MASConstraintMaker *make) {
            make.left.equalTo(self.verticalLine.mas_right).with.offset(6);
        }];
    }];


    [_topLevelDecConstraints enumerateObjectsUsingBlock:^(MASConstraint *constraint, NSUInteger idx, BOOL *stop) {
        [constraint uninstall];
    }];

    if(self.dataDeclarationViews.count == 0)
    {

    }
    else
    {
        NSMutableArray *constraints = [@[] mutableCopy];

        UIView *highestDec = self.dataDeclarationViews[0];



        UIView *lowestConstructor = self.dataDeclarationViews[_dataDeclarationViews.count - 1];
        [constraints addObjectsFromArray:[self.addTopLevelDecButton mas_updateConstraints:^(MASConstraintMaker *make) {
            make.top.equalTo(lowestConstructor.mas_bottom);
        }]];

        _topLevelDecConstraints = constraints;
    }

}

- (void) addDataDeclaration
{
    IDTDataDeclarationView *dataDeclarationView = [[IDTDataDeclarationView alloc] initAndLayout];

    [self addSubview:dataDeclarationView];
    [self.dataDeclarationViews addObject:dataDeclarationView];
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
        _addTopLevelDecButton = [UIButton buttonWithType:UIButtonTypeRoundedRect];
        [_addTopLevelDecButton setTitle:@"Add" forState:UIControlStateNormal];
        _addTopLevelDecButton.cas_styleClass = @"top-level-dec-add-button";
    }

    return _addTopLevelDecButton;
}

- (NSMutableArray *)dataDeclarationViews {
    if(!_dataDeclarationViews)
    {
        _dataDeclarationViews = [@[] mutableCopy];
    }
    return _dataDeclarationViews;
}


@end