import { CommonModule } from '@angular/common';
import {
  Component,
  ElementRef,
  HostListener,
  inject,
  signal,
  viewChild,
  viewChildren,
} from '@angular/core';
import { RouterModule } from '@angular/router';
import { AuthService, OutsideClickDirective } from '../../core';
import { UserMenuComponent } from '../user-menu/user-menu.component';

@Component({
  selector: 'app-navigation',
  standalone: true,
  imports: [CommonModule, RouterModule, UserMenuComponent],
  templateUrl: './navigation.component.html',
  styleUrl: './navigation.component.scss',
})
export class NavigationComponent extends OutsideClickDirective {
  public auth = inject(AuthService);
}
