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



- (id)initWithOptions: (NSArray*) options {
    self = [super initWithNibName:nil bundle:nil];
    if (self) {
        self.options = options;
    }

    return self;
}


- (void)viewDidLoad {
    self.preferredContentSize = CGSizeMake(200, 140);

    _mainView.tableView.delegate = self;
    _mainView.tableView.dataSource = self;

}





#pragma mark - UITableViewDataSource


- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section {
    return self.options.count;
}

- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath {
    
    UITableViewCell *contextCell = [tableView dequeueReusableCellWithIdentifier:cellIdentifier];

    if(!contextCell)
    {
        contextCell = [[UITableViewCell alloc] initWithStyle:UITableViewCellStyleDefault reuseIdentifier:cellIdentifier];
    }

    contextCell.textLabel.text = self.options[(NSUInteger) indexPath.row];

    return contextCell;
}


#pragma mark - UITableViewDelegate

- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath {
    [self.viewModel.selectionCommand execute:@(indexPath.row)];

    [tableView deselectRowAtIndexPath:indexPath animated:YES];
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