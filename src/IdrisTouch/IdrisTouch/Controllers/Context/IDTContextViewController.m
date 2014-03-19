//
// Created by Nicolai Dahl on 19/03/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTContextViewController.h"
#import "IDTContextView.h"
#import "IDTAbstractViewModel.h"
#import "IDTContextViewModel.h"

@interface IDTAbstractViewController () <UITableViewDataSource, UITableViewDelegate>



@end

@implementation IDTContextViewController {
    IDTContextView *_mainView;
    IDTContextViewModel *_viewModel;
}

- (void)viewDidLoad {
    self.preferredContentSize = CGSizeMake(200, 120);

    _mainView.tableView.delegate = self;
    _mainView.tableView.dataSource = self;

}





#pragma mark - UITableViewDataSource


- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section {
    return 2;
}

- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath {
    
    UITableViewCell *contextCell = [tableView dequeueReusableCellWithIdentifier:cellIdentifier];

    if(!contextCell)
    {
        contextCell = [[UITableViewCell alloc] initWithStyle:UITableViewCellStyleDefault reuseIdentifier:cellIdentifier];
    }

    if(indexPath.row == 0)
        contextCell.textLabel.text = @"data";
    else if(indexPath.row == 1)
        contextCell.textLabel.text = @"function";

    return contextCell;
}


#pragma mark - UITableViewDelegate

- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath {
    [self.viewModel.selectionCommand execute:@(indexPath.row)];


}

#pragma mark - Accessors

- (IDTAbstractView *)mainView {
    if(!_mainView)
    {
        _mainView = [[IDTContextView alloc] initAndLayout];
    }

    return _mainView;
}

- (IDTAbstractViewModel *)viewModel {
    if(!_viewModel)
    {
        _viewModel = [[IDTContextViewModel alloc] init];
    }

    return _viewModel;
}

- (RACCommand *)selectionCommand {
    return self.viewModel.selectionCommand;
}


@end