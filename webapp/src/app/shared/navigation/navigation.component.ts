import { CommonModule } from '@angular/common';
import { Component, effect, inject, signal } from '@angular/core';
import { RouterModule } from '@angular/router';
import {
  AuthService,
  OutsideClickDirective,
  UserService,
  WasmService,
} from '../../core';
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
  private wasm = inject(WasmService);
  constructor() {
    super();
  }
}
