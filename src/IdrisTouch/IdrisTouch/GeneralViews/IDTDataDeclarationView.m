//
// Created by Nicolai Dahl on 14/03/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTDataDeclarationView.h"
#import "IDTInferenceRuleView.h"
#import "UIColor+CASAdditions.h"

@interface IDTDataDeclarationView ()

@property (nonatomic, strong) UILabel *dataLabel;
@property (nonatomic, strong) IDTInferenceRuleView *typeDeclaration;
@property (nonatomic, strong) UILabel *whereLabel;
@property (nonatomic, strong) UIView *verticalLine;
@property (nonatomic, strong) NSMutableArray *constructors;
@property (nonatomic, strong) UIButton *addConstructorButton;


@end

@implementation IDTDataDeclarationView {
    NSArray *_constructorConstraints;
}

- (id)initAndLayout {
    self = [super initAndLayout];
    if (self) {
        [[self.addConstructorButton rac_signalForControlEvents:UIControlEventTouchUpInside] subscribeNext:^(id x) {
            [self addConstructor];
            [self updateConstraints];
        }];
    }

    return self;
}


- (void)addSubviews {

    [self addSubview:self.dataLabel];
    [self addSubview:self.verticalLine];
    [self addSubview:self.addConstructorButton];
    [self addSubview:self.typeDeclaration];
    [self addSubview:self.whereLabel];
}

- (void)defineLayout {

    [self.dataLabel mas_updateConstraintsWithLeftMarginRelativeToSuperview];
    [self.dataLabel mas_updateConstraintsWithRightMarginRelativeTo:self.typeDeclaration.mas_left];
    [self.dataLabel mas_updateConstraints:^(MASConstraintMaker *make) {
        make.centerY.equalTo(self.typeDeclaration);
    }];
    [self.dataLabel mas_updateConstraintsHeightFromStylesheet];

    [self.verticalLine mas_updateConstraintsWidthFromStylesheet];
    [self.verticalLine mas_updateConstraintsWithTopMarginRelativeTo:self.dataLabel.mas_bottom];
    [self.verticalLine mas_updateConstraints:^(MASConstraintMaker *make) {
        make.centerX.equalTo(self.dataLabel);
    }];

    [self.addConstructorButton mas_updateConstraints:^(MASConstraintMaker *make) {
        make.centerX.equalTo(self.verticalLine);
        make.top.equalTo(self.verticalLine.mas_bottom);
    }];
    [self.addConstructorButton mas_updateConstraintsWithBottomMarginRelativeToSuperview];


    [self.typeDeclaration mas_updateConstraintsWithTopMarginRelativeToSuperview];

    [self.whereLabel mas_updateConstraintsWithLeftMarginRelativeTo:self.typeDeclaration.mas_right];
    [self.whereLabel mas_updateConstraints:^(MASConstraintMaker *make) {
        make.right.lessThanOrEqualTo(self.whereLabel.superview);
        make.centerY.equalTo(self.typeDeclaration);
    }];

    [self.constructors enumerateObjectsUsingBlock:^(UIView *constructor, NSUInteger idx, BOOL *stop) {

        UIView *topNeighbor;
        if(idx == 0)
        {
            topNeighbor = self.typeDeclaration;
        }
        else
        {
            topNeighbor = _constructors[idx-1];
        }

        [constructor mas_updateConstraints:^(MASConstraintMaker *make) {
            make.top.equalTo(topNeighbor.mas_bottom).with.offset(10);
            make.left.equalTo(self.verticalLine.mas_right).with.offset(6);
        }];
    }];

    [_constructorConstraints enumerateObjectsUsingBlock:^(MASConstraint *constraint, NSUInteger idx, BOOL *stop) {
        [constraint uninstall];
    }];

    if(self.constructors.count == 0)
    {
        _constructorConstraints = [self.addConstructorButton mas_updateConstraints:^(MASConstraintMaker *make) {
            make.top.equalTo(self.typeDeclaration.mas_bottom);
        }];
    }
    else
    {
        UIView *lowestConstructor = self.constructors[_constructors.count - 1];
        _constructorConstraints = [self.addConstructorButton mas_updateConstraints:^(MASConstraintMaker *make) {
            make.top.equalTo(lowestConstructor.mas_bottom);
        }];
    }

}



- (void) addConstructor {

    IDTInferenceRuleView *newConstructor = [[IDTInferenceRuleView alloc] initAndLayout];
    newConstructor.cas_styleClass = @"data-dec-constructor";

    [self addSubview:newConstructor];
    [self.constructors addObject:newConstructor];

}

#pragma mark - Accessors

- (UILabel *)dataLabel {
    if(!_dataLabel)
    {
        _dataLabel = [UILabel new];
        _dataLabel.cas_styleClass = @"data-dec-data-label";
        _dataLabel.text = @"data";
    }

    return _dataLabel;
}

- (IDTInferenceRuleView *)typeDeclaration {
    if(!_typeDeclaration)
    {
        _typeDeclaration = [[IDTInferenceRuleView alloc] initAndLayout];
        _typeDeclaration.cas_styleClass = @"data-dec-type-inference-rule";

    }

    return _typeDeclaration;
}

- (UILabel *)whereLabel {
    if(!_whereLabel)
    {
        _whereLabel = [UILabel new];
        _whereLabel.cas_styleClass = @"data-dec-where-label";
        _whereLabel.text = @"where";
    }
    return _whereLabel;
}

- (UIView *)verticalLine {
    if(!_verticalLine)
    {
        _verticalLine = [[UIView alloc] init];
        _verticalLine.cas_styleClass = @"data-dec-vertical-line";
    }
    return _verticalLine;
}


- (NSMutableArray *)constructors {
    if(!_constructors)
    {
        _constructors = [@[] mutableCopy];
    }
    return _constructors;
}

- (UIButton *)addConstructorButton {
    if(!_addConstructorButton)
    {
        _addConstructorButton = [UIButton buttonWithType:UIButtonTypeCustom];
        [_addConstructorButton setImage:[UIImage imageNamed:@"add_button"] forState:UIControlStateNormal];
        _addConstructorButton.cas_styleClass = @"data-dec-add-button";
    }

    return _addConstructorButton;
}


@end