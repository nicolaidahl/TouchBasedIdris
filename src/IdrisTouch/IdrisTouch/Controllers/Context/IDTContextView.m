//
// Created by Nicolai Dahl on 19/03/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTContextView.h"


@interface IDTContextView ()



@end

@implementation IDTContextView {

}

- (void)addSubviews {
    [self addSubview:self.tableView];
}

- (void)defineLayout {
    [self.tableView mas_updateConstraints:^(MASConstraintMaker *make) {
        make.edges.equalTo(self);
    }];
}

- (UITableView *)tableView {
    if(!_tableView)
    {
        _tableView = [[UITableView alloc] initWithFrame:CGRectZero style:UITableViewStylePlain];
    }

    return _tableView;
}


@end