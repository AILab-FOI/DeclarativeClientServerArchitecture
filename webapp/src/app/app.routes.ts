import { Routes } from '@angular/router';
import { authGuard } from './core';
import { HomeComponent, LoginComponent } from './features';

export const routes: Routes = [
  { path: '', redirectTo: 'home', pathMatch: 'full' },
  { path: 'home', component: HomeComponent, title: 'Home' },
  { path: 'login', component: LoginComponent, title: 'Login' },
  {
    path: 'dashboard',
    loadComponent: () => import('./features').then((c) => c.DashboardComponent),
    title: 'Dashboard',
    canActivate: [authGuard],
    children: [],
  },
  {
    path: 'courses',
    title: 'Courses',
    loadComponent: () => import('./features').then((c) => c.CoursesComponent),
    canActivate: [authGuard],
  },
  {
    path: 'course/:id',
    title: 'Course',
    loadComponent: () => import('./features').then((c) => c.CourseComponent),
    canActivate: [authGuard],
  },
];
