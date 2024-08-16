import { CommonModule } from '@angular/common';
import { Component, effect, inject, signal } from '@angular/core';
import { InputComponent } from '../../shared';
import { AuthService } from '../../core';
import { Router } from '@angular/router';

export type ErrorLogin = {
  is: boolean;
  value: string;
};

@Component({
  selector: 'app-login',
  standalone: true,
  imports: [CommonModule, InputComponent],
  templateUrl: './login.component.html',
  styleUrl: './login.component.scss',
})
export class LoginComponent {
  email = signal<string>('');
  password = signal<string>('');
  error = signal<ErrorLogin>({
    is: false,
    value: '',
  });
  private auth = inject(AuthService);
  private router = inject(Router);

  submit(): void {
    this.auth.login(this.email(), this.password()).then((isLogedIn) => {
      if (isLogedIn.status) {
        this.router.navigate(['dashboard']);
      } else {
        this.error.set({ is: true, value: isLogedIn.data });
      }
    });
  }
}
