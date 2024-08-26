import { Routes } from '@angular/router';
import { authGuard } from './core';
import { HomeComponent, LoginComponent } from './features';
import { workerGuard } from './core/guards/worker.guard';
import { studentGuard } from './core/guards/student.guard';

export const routes: Routes = [
  { path: '', redirectTo: 'home', pathMatch: 'full' },
  { path: 'home', component: HomeComponent, title: 'Home' },
  { path: 'login', component: LoginComponent, title: 'Login' },
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
    path: 'worker',
    title: 'Courses',
    loadComponent: () => import('./features').then((c) => c.WorkerComponent),
    canActivate: [authGuard, workerGuard],
    children: [
      {
        path: 'course/:id',
        title: 'Course',
        loadComponent: () =>
          import('./features').then((c) => c.WorkerCourseComponent),
      },
    ],
  },

  {
    path: 'student',
    title: 'Courses',
    loadComponent: () => import('./features').then((c) => c.StudentComponent),
    canActivate: [authGuard, studentGuard],
    children: [
      {
        path: 'course/:id',
        title: 'Course',
        loadComponent: () =>
          import('./features').then((c) => c.StudentCourseComponent),
      },
    ],
  },
];
