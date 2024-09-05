import { CommonModule } from '@angular/common';
import { Component, effect, inject, signal } from '@angular/core';
import { InputComponent } from '../../shared';
import { AuthService } from '../../core';
import { Router } from '@angular/router';
import { ToastrService } from 'ngx-toastr';

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
  private toast = inject(ToastrService);

  submit(): void {
    this.auth.login(this.email(), this.password()).then((isLogedIn) => {
      if (isLogedIn.status) {
        this.toast.success('Logged in!');
        this.router.navigate(['dashboard']);
      } else {
        this.toast.error(isLogedIn.data);
      }
    });
  }
}
