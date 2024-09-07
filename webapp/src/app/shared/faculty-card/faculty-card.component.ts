import { Component, inject, input } from '@angular/core';
import { Fakultet } from '../../core';
import { Router } from '@angular/router';
import { NgOptimizedImage } from '@angular/common';

@Component({
  selector: 'app-faculty-card',
  standalone: true,
  imports: [NgOptimizedImage],
  templateUrl: './faculty-card.component.html',
  styleUrl: './faculty-card.component.scss',
})
export class FacultyCardComponent {
  public faculty = input.required<Fakultet>();
  private router = inject(Router);

  goTo(): void {
    this.router.navigate(['faculty', this.faculty().id]);
  }
}
