//
// Created by Nicolai Dahl on 14/03/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTDashedTextField.h"


@implementation IDTDashedTextField {

    CAShapeLayer *_border;
}

- (id)initWithFrame:(CGRect)frame {
    self = [super initWithFrame:frame];
    if (self) {
        _border = [CAShapeLayer layer];
        _border.strokeColor = [UIColor colorWithRed:67/255.0f green:37/255.0f blue:83/255.0f alpha:1].CGColor;
        _border.lineWidth = 2;
        _border.fillColor = nil;
        _border.lineDashPattern = @[@8, @4];
        [self.layer addSublayer:_border];
    }

    return self;
}


- (void)layoutSubviews {
    [super layoutSubviews];

    _border.path = [UIBezierPath bezierPathWithRoundedRect:self.bounds cornerRadius:8.f].CGPath;
    _border.frame = self.bounds;
    

    
    
}


@end