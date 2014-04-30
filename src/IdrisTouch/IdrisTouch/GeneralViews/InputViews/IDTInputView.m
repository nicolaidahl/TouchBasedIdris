//
// Created by Nicolai Dahl on 17/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTInputView.h"


@implementation IDTInputView {

}


- (id)initWithFrame:(CGRect)frame {
    self = [super initWithFrame:frame];
    if (self) {
        UITapGestureRecognizer *recognizer = [[UITapGestureRecognizer alloc] init];
        recognizer.numberOfTapsRequired = 2;

//        UISwipeGestureRecognizer *recognizer = [[UISwipeGestureRecognizer alloc] initWithTarget:nil action:nil];
//        [recognizer setDirection:(UISwipeGestureRecognizerDirectionDown)];
        [self addGestureRecognizer:recognizer];
        self.swipeDownSignal = [recognizer rac_gestureSignal];
    }

    return self;
}


@end