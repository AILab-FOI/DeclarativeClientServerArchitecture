import { CommonModule } from '@angular/common';
import { Component, inject } from '@angular/core';
import { RouterModule } from '@angular/router';
import { AuthService, OutsideClickDirective, UserService } from '../../core';
import { UserMenuComponent } from '../user-menu/user-menu.component';

@Component({
  selector: 'app-navigation',
  standalone: true,
  imports: [CommonModule, RouterModule, UserMenuComponent],
  templateUrl: './navigation.component.html',
  styleUrl: './navigation.component.scss',
})
export class NavigationComponent extends OutsideClickDirective {
  public user = inject(UserService);
  public auth = inject(AuthService);
  constructor() {
    super();
  }
}
