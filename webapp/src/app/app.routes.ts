import { Routes } from '@angular/router';
import { authGuard } from './core';
import { HomeComponent, LoginComponent } from './features';
import { FacultyComponent } from './features/faculty/faculty.component';
import { DepartmentComponent } from './features/department/department.component';

export const routes: Routes = [
  { path: '', redirectTo: 'home', pathMatch: 'full' },
  { path: 'home', component: HomeComponent, title: 'Home' },
  { path: 'login', component: LoginComponent, title: 'Login' },
  { path: 'faculty/:id', component: FacultyComponent, title: 'Faculty' },
  { path: 'department/:id', component: DepartmentComponent, title: 'Faculty' },
  {
    path: 'dashboard',
    loadComponent: () => import('./features').then((c) => c.DashboardComponent),
    title: 'Dashboard',
    canActivate: [authGuard],
  },
  {
    path: 'courses',
    title: 'Courses',
    loadComponent: () => import('./features').then((c) => c.CoursesComponent),
    canActivate: [authGuard],
  },
  {
    path: 'profile/:id',
    title: 'Profile',
    loadComponent: () => import('./features').then((c) => c.ProfileComponent),
    canActivate: [authGuard],
  },
  {
    path: 'course/:id',
    title: 'Course',
    loadComponent: () => import('./features').then((c) => c.CourseComponent),
    canActivate: [authGuard],
  },
  {
    path: 'content/:id',
    title: 'Content',
    loadComponent: () => import('./features').then((c) => c.ContentComponent),
    canActivate: [authGuard],
  },
];
