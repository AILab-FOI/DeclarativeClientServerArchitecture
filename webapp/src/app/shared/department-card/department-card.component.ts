import { Component, inject, input } from '@angular/core';
import { Katedra } from '../../core';
import { Router } from '@angular/router';

@Component({
  selector: 'app-department-card',
  standalone: true,
  imports: [],
  templateUrl: './department-card.component.html',
  styleUrl: './department-card.component.scss',
})
export class DepartmentCardComponent {
  public department = input.required<Katedra>();

  private router = inject(Router);

  goTo(): void {
    this.router.navigate(['department', this.department().id]);
  }
}
