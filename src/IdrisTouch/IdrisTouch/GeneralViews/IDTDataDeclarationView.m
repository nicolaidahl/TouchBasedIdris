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
@property (nonatomic, strong) NSArray *constructors;


@end

@implementation IDTDataDeclarationView {

}

- (void)addSubviews {

    [self addSubview:self.dataLabel];
    [self addSubview:self.typeDeclaration];
    [self addSubview:self.whereLabel];

    for (IDTInferenceRuleView *inferenceRuleView in self.constructors) {
        [self addSubview:inferenceRuleView];
    }
}

- (void)defineLayout {

    [self.dataLabel mas_updateConstraintsWithLeftMarginRelativeToSuperview];
    [self.dataLabel mas_updateConstraintsWithRightMarginRelativeTo:self.typeDeclaration.mas_left];
    [self.dataLabel mas_updateConstraints:^(MASConstraintMaker *make) {
        make.centerY.equalTo(self.typeDeclaration);
    }];
    [self mas_updateConstraintsHeightFromStylesheet];

    [self.typeDeclaration mas_updateConstraintsWithTopMarginRelativeToSuperview];

    [self.whereLabel mas_updateConstraintsWithLeftMarginRelativeTo:self.typeDeclaration.mas_right];
    [self.whereLabel mas_updateConstraints:^(MASConstraintMaker *make) {
        make.right.lessThanOrEqualTo(self.whereLabel.superview);
        make.centerY.equalTo(self.typeDeclaration);
    }];


}

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

- (NSArray *)constructors {
    if(!_constructors)
    {
        _constructors = [@[] mutableCopy];
    }
    return _constructors;
}


@end