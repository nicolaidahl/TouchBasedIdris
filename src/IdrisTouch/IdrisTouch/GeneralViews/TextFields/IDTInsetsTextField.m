//
// Created by Nicolai Dahl on 25/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTInsetsTextField.h"


@implementation IDTInsetsTextField {

}


// placeholder position
- (CGRect)textRectForBounds:(CGRect)bounds {
    return CGRectInset( bounds , 10 , 10 );
}

// text position
- (CGRect)editingRectForBounds:(CGRect)bounds {
    return CGRectInset( bounds , 10 , 10 );
}

@end