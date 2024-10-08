import { Component, inject, input } from '@angular/core';
import { Korisnik } from '../../core';
import { Router } from '@angular/router';
import { NgOptimizedImage } from '@angular/common';

@Component({
  selector: 'app-user-card',
  standalone: true,
  imports: [NgOptimizedImage],
  templateUrl: './user-card.component.html',
  styleUrl: './user-card.component.scss',
})
export class UserCardComponent {
  public user = input.required<Korisnik>();
  public clickable = input<boolean>(true);
  private router = inject(Router);
  goTo(): void {
    if (this.clickable()) {
      this.router.navigate(['profile', this.user().id]);
    }
  }
}
