//
//  IDTAppDelegate.h
//  IdrisTouch
//
//  Created by Nicolai Dahl on 05/03/14.
//  Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface IDTAppDelegate : UIResponder <UIApplicationDelegate>

@property (strong, nonatomic) UIWindow *window;

@property (readonly, strong, nonatomic) NSManagedObjectContext *managedObjectContext;
@property (readonly, strong, nonatomic) NSManagedObjectModel *managedObjectModel;
@property (readonly, strong, nonatomic) NSPersistentStoreCoordinator *persistentStoreCoordinator;

- (void)saveContext;
- (NSURL *)applicationDocumentsDirectory;

@end
