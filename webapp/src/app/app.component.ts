import { Component, inject } from '@angular/core';
import { RouterOutlet } from '@angular/router';
import { NavigationComponent } from './shared';
import { UserService } from './core';

@Component({
  selector: 'app-root',
  standalone: true,
  imports: [RouterOutlet, NavigationComponent],
  templateUrl: './app.component.html',
  styleUrl: './app.component.scss',
})
export class AppComponent {
  title = 'client';
  user = inject(UserService);
  constructor() {
    console.log('AS');
  }
}
