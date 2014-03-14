//
// Created by Nicolai Dahl on 05/03/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTMainView.h"
#import "IDTInferenceRuleView.h"
#import "IDTDataDeclarationView.h"

@interface IDTMainView ()

@property (nonatomic, strong) IDTDataDeclarationView *dataDeclarationView;

@end

@implementation IDTMainView {

}

- (void)addSubviews {
    [self addSubview:self.dataDeclarationView];

}

- (void)defineLayout {

    [self.dataDeclarationView mas_updateConstraints:^(MASConstraintMaker *make) {
        make.centerX.equalTo(self);
        make.top.equalTo(self).with.offset(20);
    }];


}


- (IDTDataDeclarationView *)dataDeclarationView {
    if(!_dataDeclarationView)
    {
        _dataDeclarationView = [[IDTDataDeclarationView alloc] initAndLayout];
    }
    return _dataDeclarationView;
}


@end