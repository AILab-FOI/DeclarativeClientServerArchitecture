import { Component, inject, input, model } from '@angular/core';
import { AuthService, Korisnik } from '../../core';
import { CommonModule } from '@angular/common';
import { RouterModule } from '@angular/router';

@Component({
  selector: 'app-user-menu',
  standalone: true,
  imports: [CommonModule, RouterModule],
  templateUrl: './user-menu.component.html',
  styleUrl: './user-menu.component.scss',
})
export class UserMenuComponent {
  public auth = inject(AuthService);
  public userMenu = model<boolean>(false);
  public user = input.required<Korisnik>();
}
